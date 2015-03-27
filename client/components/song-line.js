import React from 'react';

export default React.createClass({

    render: function () {

        return (
            // adding those as metadata allows to ues them
            // for styling
            <span data-chord={this.props.data.chordName}
                  data-lyrics={this.props.data.content}>
                <b>{this.props.data.chordName}</b>
                {this.props.data.content}
            </span>
        );

    }

});
