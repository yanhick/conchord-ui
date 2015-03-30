import React from 'react';

import SongMeta from './song-meta';
import SongContent from './song-content';
import SongToolbar from './song-toolbar';
import Chords from './chords';
import Header from './header';

export default React.createClass({

    render: function () {


        return (
            <div>
                <Header onGoHome={this.props.onGoHome}>
                    <SongToolbar {...this.props}/>
                </Header>
                <main>
                    <SongMeta data={this.props.data.meta} />
                    <Chords
                        showChords={this.props.showChords}
                        data={this.props.data.chords} />
                    <SongContent data={this.props.data.content}
                                 showDuplicatedChoruses={this.props.showDuplicatedChoruses}
                                 fontSize={this.props.fontSize} />
                </main>
            </div>
        );
    }

});
