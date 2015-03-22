import React from 'react';

import Search from './search';

export default React.createClass({

    render: function () {
        return (
            <Search onSelect={this.props.onSelect} />
        );
    }

});

