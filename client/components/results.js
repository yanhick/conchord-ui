import React from 'react';
import Result from './result';

export default React.createClass({

    render: function () {

        const resultList = this.props.data.map((result) => {
            return (
                <Result key={result.id} data={result} onSelect={this.props.onSelect} />
            );
        });

        return (
            <ul className="song-metas">
                {resultList}
            </ul>
        );
    }

});

